document.addEventListener('DOMContentLoaded', () => {
    const canvas = document.getElementById('gameCanvas');
    const ctx = canvas.getContext('2d');
    const messageElement = document.getElementById('message');
    const timerElement = document.getElementById('timer');
    const resetButton = document.getElementById('resetButton');
    const giveUpButton = document.getElementById('giveUpButton');
    const difficultySelect = document.getElementById('difficulty');
    
    // Variabler for tidtaking
    let startTime;
    let timerInterval;
    let gameRunning = false;
    let gameCompleted = false;
    let gameGivenUp = false;
    let shortestPath = [];
    
    // Variabler for fyrverkeri
    let fireworks = [];
    let fireworkInterval;
    let animationFrameId = null;
    
    // 7-segment display konfigurasjon
    const digitSegments = [
        [1, 1, 1, 1, 1, 1, 0], // 0
        [0, 1, 1, 0, 0, 0, 0], // 1
        [1, 1, 0, 1, 1, 0, 1], // 2
        [1, 1, 1, 1, 0, 0, 1], // 3
        [0, 1, 1, 0, 0, 1, 1], // 4
        [1, 0, 1, 1, 0, 1, 1], // 5
        [1, 0, 1, 1, 1, 1, 1], // 6
        [1, 1, 1, 0, 0, 0, 0], // 7
        [1, 1, 1, 1, 1, 1, 1], // 8
        [1, 1, 1, 1, 0, 1, 1]  // 9
    ];
    
    // Opprett 7-segment display
    function createTimerDisplay() {
        const timerElement = document.getElementById('timer');
        timerElement.innerHTML = '';
        
        // Minutter
        for (let i = 0; i < 2; i++) {
            createDigit(timerElement);
        }
        
        // Kolon
        const colon1 = document.createElement('span');
        colon1.className = 'colon';
        colon1.textContent = ':';
        timerElement.appendChild(colon1);
        
        // Sekunder
        for (let i = 0; i < 2; i++) {
            createDigit(timerElement);
        }
        
        // Kolon/punktum for hundredeler
        const colon2 = document.createElement('span');
        colon2.className = 'dot';
        colon2.textContent = '.';
        timerElement.appendChild(colon2);
        
        // Hundredeler
        for (let i = 0; i < 2; i++) {
            createDigit(timerElement);
        }
        
        // Sett startverdien til 00:00.00
        updateDigitDisplay('00:00.00');
    }
    
    // Opprett en enkelt siffer
    function createDigit(parent) {
        const digit = document.createElement('div');
        digit.className = 'digit';
        
        // Opprett de 7 segmentene (a-g)
        const segments = ['a', 'b', 'c', 'd', 'e', 'f', 'g'];
        segments.forEach(seg => {
            const segment = document.createElement('div');
            segment.className = `segment segment-${seg}`;
            digit.appendChild(segment);
        });
        
        parent.appendChild(digit);
    }
    
    // Oppdater display med tid
    function updateDigitDisplay(timeString) {
        // Fjern kolon og punktum
        const digits = timeString.replace(/[:\.]/g, '');
        const digitElements = document.querySelectorAll('.digit');
        
        // Oppdater hvert siffer
        for (let i = 0; i < digits.length; i++) {
            const digit = parseInt(digits[i], 10);
            const segments = digitElements[i].querySelectorAll('.segment');
            
            // Slå på/av segmenter basert på siffer
            for (let j = 0; j < segments.length; j++) {
                if (digitSegments[digit][j] === 1) {
                    segments[j].classList.add('on');
                } else {
                    segments[j].classList.remove('on');
                }
            }
        }
    }
    
    
    // Konstanter for labyrinten
    const WALL = 0;
    const PATH = 1;
    const EXIT = 3;
    
    // Variabler for labyrintstørrelse
    let MAZE_WIDTH = 20;
    let MAZE_HEIGHT = 20;
    let cellSize = 25;
    
    // Labyrint-array
    let maze = [];
    
    // Sett vanskelighetsgrad og tilpass størrelsen
    function setDifficulty(difficulty) {
        // Sett basisstørrelse for labyrinten basert på vanskelighetsgrad
        switch(difficulty) {
            case 'easy':
                MAZE_WIDTH = MAZE_HEIGHT = 21; // Oddetall for labyrintalgoritmen
                break;
            case 'normal':
                MAZE_WIDTH = MAZE_HEIGHT = 41; // Oddetall for labyrintalgoritmen
                break;
            case 'hard':
                MAZE_WIDTH = MAZE_HEIGHT = 61; // Oddetall for labyrintalgoritmen
                break;
        }
        
        // Juster canvas-størrelsen basert på vindusstørrelse
        adjustCanvasSize();
    }
    
    // Spillerens posisjon
    let playerX, playerY;
    
    // Målposisjon
    let exitX, exitY;
    
    // Generer en tilfeldig labyrint med rekursiv backtracking
    function generateMaze() {
        // Initialiser labyrinten med vegger
        maze = Array(MAZE_HEIGHT).fill().map(() => Array(MAZE_WIDTH).fill(WALL));
        
        // Sørg for at ytterkanten er vegger
        for (let y = 0; y < MAZE_HEIGHT; y++) {
            for (let x = 0; x < MAZE_WIDTH; x++) {
                if (y === 0 || y === MAZE_HEIGHT - 1 || x === 0 || x === MAZE_WIDTH - 1) {
                    maze[y][x] = WALL;
                }
            }
        }
        
        // Start fra et tilfeldig punkt (må være oddetall for å unngå å starte på en kant)
        const startX = Math.floor(Math.random() * (MAZE_WIDTH - 4) / 2) * 2 + 2;
        const startY = Math.floor(Math.random() * (MAZE_HEIGHT - 4) / 2) * 2 + 2;
        
        // Rekursiv funksjon for å lage stier
        function carve(x, y) {
            // Merk denne cellen som en sti
            maze[y][x] = PATH;
            
            // Retninger: [dx, dy]
            const directions = [
                [0, -2], // opp
                [2, 0],  // høyre
                [0, 2],  // ned
                [-2, 0]  // venstre
            ];
            
            // Stokk om på retningene for å få en tilfeldig labyrint
            shuffleArray(directions);
            
            // Prøv hver retning
            for (const [dx, dy] of directions) {
                const nx = x + dx;
                const ny = y + dy;
                
                // Sjekk om den nye posisjonen er innenfor labyrinten og er en vegg
                if (nx > 0 && nx < MAZE_WIDTH - 1 && ny > 0 && ny < MAZE_HEIGHT - 1 && maze[ny][nx] === WALL) {
                    // Sjekk om å lage en sti her vil skape et 2x2 hulrom
                    const midX = x + dx/2;
                    const midY = y + dy/2;
                    
                    // Lag en sti mellom cellene hvis det ikke skaper et 2x2 hulrom
                    maze[midY][midX] = PATH;
                    
                    // Fortsett rekursivt
                    carve(nx, ny);
                }
            }
        }
        
        // Start rekursiv generering
        carve(startX, startY);
        
        // Legg til flere stier for å gjøre labyrinten lettere
        addExtraPaths();
        
        // Sett startpunkt i midten
        playerX = Math.floor(MAZE_WIDTH / 2);
        playerY = Math.floor(MAZE_HEIGHT / 2);
        
        // Sørg for at startpunktet er en sti
        maze[playerY][playerX] = PATH;
        
        // Lag en sti rundt startpunktet
        for (let dy = -1; dy <= 1; dy++) {
            for (let dx = -1; dx <= 1; dx++) {
                if (playerY + dy > 0 && playerY + dy < MAZE_HEIGHT - 1 && 
                    playerX + dx > 0 && playerX + dx < MAZE_WIDTH - 1) {
                    maze[playerY + dy][playerX + dx] = PATH;
                }
            }
        }
        
        // Sett utgangen på en tilfeldig kant
        placeExit();
    }
    
    // Funksjon for å stokke om på et array (Fisher-Yates algoritme)
    function shuffleArray(array) {
        for (let i = array.length - 1; i > 0; i--) {
            const j = Math.floor(Math.random() * (i + 1));
            [array[i], array[j]] = [array[j], array[i]];
        }
        return array;
    }
    
    // Legg til noen ekstra stier for å gjøre labyrinten mer interessant
    function addExtraPaths() {
        const extraPaths = Math.floor((MAZE_WIDTH * MAZE_HEIGHT) * 0.05); // 5% ekstra stier
        
        for (let i = 0; i < extraPaths; i++) {
            const x = Math.floor(Math.random() * (MAZE_WIDTH - 2)) + 1;
            const y = Math.floor(Math.random() * (MAZE_HEIGHT - 2)) + 1;
            
            // Sjekk om å legge til en sti her vil skape et 2x2 hulrom
            if (maze[y][x] === WALL && !wouldCreateOpenSpace(x, y)) {
                maze[y][x] = PATH;
            }
        }
    }
    
    // Sjekk om å gjøre en celle til sti vil skape et 2x2 åpent område
    function wouldCreateOpenSpace(x, y) {
        // Sjekk alle mulige 2x2 områder som denne cellen kan være en del av
        for (let dy = -1; dy <= 0; dy++) {
            for (let dx = -1; dx <= 0; dx++) {
                // Sjekk om alle fire hjørner i et 2x2 område ville være stier
                const topLeft = (y + dy >= 0 && x + dx >= 0) ? maze[y + dy][x + dx] : WALL;
                const topRight = (y + dy >= 0 && x + dx + 1 < MAZE_WIDTH) ? maze[y + dy][x + dx + 1] : WALL;
                const bottomLeft = (y + dy + 1 < MAZE_HEIGHT && x + dx >= 0) ? maze[y + dy + 1][x + dx] : WALL;
                const bottomRight = (x === x + dx + 1 && y === y + dy + 1) ? PATH : 
                                   ((y + dy + 1 < MAZE_HEIGHT && x + dx + 1 < MAZE_WIDTH) ? maze[y + dy + 1][x + dx + 1] : WALL);
                
                // Teller antall stier (inkludert den vi vurderer å legge til)
                let pathCount = 0;
                if (topLeft === PATH) pathCount++;
                if (topRight === PATH) pathCount++;
                if (bottomLeft === PATH) pathCount++;
                if (bottomRight === PATH) pathCount++;
                if (x === x + dx + 1 && y === y + dy + 1) pathCount++; // Cellen vi vurderer
                
                // Hvis alle fire celler ville være stier, returner true
                if (pathCount >= 3) {
                    return true;
                }
            }
        }
        return false;
    }
    
    // Plasser utgangen på en tilfeldig kant
    function placeExit() {
        // Velg en tilfeldig side (0=topp, 1=høyre, 2=bunn, 3=venstre)
        const side = Math.floor(Math.random() * 4);
        
        let x, y;
        
        switch (side) {
            case 0: // topp
                x = Math.floor(Math.random() * (MAZE_WIDTH - 2)) + 1;
                y = 1;
                break;
            case 1: // høyre
                x = MAZE_WIDTH - 2;
                y = Math.floor(Math.random() * (MAZE_HEIGHT - 2)) + 1;
                break;
            case 2: // bunn
                x = Math.floor(Math.random() * (MAZE_WIDTH - 2)) + 1;
                y = MAZE_HEIGHT - 2;
                break;
            case 3: // venstre
                x = 1;
                y = Math.floor(Math.random() * (MAZE_HEIGHT - 2)) + 1;
                break;
        }
        
        // Sett utgangen
        maze[y][x] = EXIT;
        exitX = x;
        exitY = y;
        
        // Sørg for at det er en sti til utgangen
        if (side === 0) maze[y+1][x] = PATH;
        else if (side === 1) maze[y][x-1] = PATH;
        else if (side === 2) maze[y-1][x] = PATH;
        else if (side === 3) maze[y][x+1] = PATH;
    }
    
    // Tegn labyrinten
    function drawMaze() {
        for (let y = 0; y < maze.length; y++) {
            for (let x = 0; x < maze[y].length; x++) {
                // Sjekk om cellen er på den korteste stien
                const isOnShortestPath = shortestPath.some(cell => cell.x === x && cell.y === y);
                
                if (isOnShortestPath && gameGivenUp) {
                    ctx.fillStyle = '#f1c40f'; // Gul for korteste vei
                } else {
                    switch (maze[y][x]) {
                        case 0: // Vegg
                            ctx.fillStyle = '#333';
                            break;
                        case 1: // Sti
                            ctx.fillStyle = '#fff';
                            break;
                        case 3: // Mål
                            ctx.fillStyle = '#27ae60';
                            break;
                        default:
                            ctx.fillStyle = '#fff';
                    }
                }
                ctx.fillRect(x * cellSize, y * cellSize, cellSize, cellSize);
                
                // Tegn ramme rundt hver celle
                ctx.strokeStyle = '#ccc';
                ctx.strokeRect(x * cellSize, y * cellSize, cellSize, cellSize);
            }
        }
    }
    
    // Tegn spilleren
    function drawPlayer() {
        // Tegn svart omriss rundt hodet
        ctx.strokeStyle = '#000';
        ctx.lineWidth = Math.max(1, cellSize / 20);
        ctx.beginPath();
        ctx.arc(
            playerX * cellSize + cellSize / 2,
            playerY * cellSize + cellSize / 2,
            cellSize / 2 - 2,
            0,
            Math.PI * 2
        );
        ctx.stroke();
        
        // Fyll hodet med gul farge
        ctx.fillStyle = '#f1c40f';
        ctx.beginPath();
        ctx.arc(
            playerX * cellSize + cellSize / 2,
            playerY * cellSize + cellSize / 2,
            cellSize / 2 - 2,
            0,
            Math.PI * 2
        );
        ctx.fill();
        
        // Tegn ansikt på spilleren
        ctx.fillStyle = '#000';
        // Øyne - størrelse tilpasses cellestørrelsen
        const eyeSize = Math.max(1, cellSize / 12);
        const eyeOffset = Math.max(2, cellSize / 8);
        
        ctx.beginPath();
        ctx.arc(
            playerX * cellSize + cellSize / 2 - eyeOffset,
            playerY * cellSize + cellSize / 2 - eyeOffset,
            eyeSize,
            0,
            Math.PI * 2
        );
        ctx.arc(
            playerX * cellSize + cellSize / 2 + eyeOffset,
            playerY * cellSize + cellSize / 2 - eyeOffset,
            eyeSize,
            0,
            Math.PI * 2
        );
        ctx.fill();
        
        // Munn - tilpasses spillets tilstand
        ctx.strokeStyle = '#000';
        ctx.lineWidth = Math.max(1, cellSize / 25);
        
        const mouthY = playerY * cellSize + cellSize / 2 + eyeOffset;
        const mouthWidth = Math.max(6, cellSize / 3);
        
        if (gameCompleted) {
            // Smilende munn når spillet er fullført
            ctx.beginPath();
            ctx.arc(
                playerX * cellSize + cellSize / 2,
                mouthY,
                mouthWidth / 2,
                0,
                Math.PI
            );
            ctx.stroke();
        } else if (gameGivenUp) {
            // Sur munn når spilleren gir opp
            ctx.beginPath();
            ctx.arc(
                playerX * cellSize + cellSize / 2,
                mouthY + mouthWidth / 2,
                mouthWidth / 2,
                Math.PI,
                Math.PI * 2
            );
            ctx.stroke();
        } else {
            // Horisontal strek som munn under vanlig spill
            ctx.beginPath();
            ctx.moveTo(
                playerX * cellSize + cellSize / 2 - mouthWidth / 2,
                mouthY
            );
            ctx.lineTo(
                playerX * cellSize + cellSize / 2 + mouthWidth / 2,
                mouthY
            );
            ctx.stroke();
        }
    }
    
    // Oppdater spillet
    function update() {
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        drawMaze();
        drawPlayer();
        
        // Sjekk om spilleren har nådd målet
        if (playerX === exitX && playerY === exitY && !gameCompleted) {
            messageElement.textContent = 'Gratulerer! Du fant veien ut!';
            stopTimer();
            gameCompleted = true;
            startFireworks();
        }
        
        // Tegn fyrverkeri hvis spillet er fullført
        if (gameCompleted) {
            updateFireworks();
            drawFireworks();
        }
        
        // Fortsett animasjonsløkken
        if (gameCompleted) {
            requestAnimationFrame(update);
        }
    }
    
    // Fyrverkeri-funksjoner
    function startFireworks() {
        // Opprett 10 fyrverkeri til å begynne med
        for (let i = 0; i < 10; i++) {
            createFirework();
        }
        
        // Legg til nye fyrverkeri med jevne mellomrom
        fireworkInterval = setInterval(() => {
            if (fireworks.length < 20) { // Begrens antall samtidige fyrverkeri
                createFirework();
            }
        }, 300);
    }
    
    function createFirework() {
        const x = Math.random() * canvas.width;
        const y = canvas.height;
        const targetY = 50 + Math.random() * (canvas.height * 0.6);
        const hue = Math.floor(Math.random() * 360);
        
        fireworks.push({
            x: x,
            y: y,
            targetY: targetY,
            speed: 3 + Math.random() * 4,
            particles: [],
            hue: hue,
            exploded: false
        });
    }
    
    function updateFireworks() {
        for (let i = fireworks.length - 1; i >= 0; i--) {
            const fw = fireworks[i];
            
            if (!fw.exploded) {
                // Beveg fyrverkeriet oppover
                fw.y -= fw.speed;
                
                // Sjekk om fyrverkeriet har nådd målhøyden
                if (fw.y <= fw.targetY) {
                    explodeFirework(fw);
                    fw.exploded = true;
                }
            } else {
                // Oppdater partikler
                for (let j = fw.particles.length - 1; j >= 0; j--) {
                    const p = fw.particles[j];
                    p.x += p.vx;
                    p.y += p.vy;
                    p.vy += 0.05; // Gravitasjon
                    p.life -= 0.02;
                    
                    // Fjern døde partikler
                    if (p.life <= 0) {
                        fw.particles.splice(j, 1);
                    }
                }
                
                // Fjern fyrverkeri uten partikler
                if (fw.particles.length === 0) {
                    fireworks.splice(i, 1);
                }
            }
        }
        
        // Legg til nye fyrverkeri hvis det er for få
        if (gameCompleted && fireworks.length < 3) {
            createFirework();
        }
    }
    
    function explodeFirework(fw) {
        const particleCount = 80 + Math.floor(Math.random() * 50);
        
        for (let i = 0; i < particleCount; i++) {
            const angle = Math.random() * Math.PI * 2;
            const speed = 1 + Math.random() * 3;
            
            fw.particles.push({
                x: fw.x,
                y: fw.y,
                vx: Math.cos(angle) * speed,
                vy: Math.sin(angle) * speed,
                life: 0.7 + Math.random() * 0.5,
                color: `hsl(${fw.hue}, 100%, ${50 + Math.random() * 30}%)`
            });
        }
    }
    
    function drawFireworks() {
        for (const fw of fireworks) {
            if (!fw.exploded) {
                // Tegn raketten
                ctx.beginPath();
                ctx.fillStyle = `hsl(${fw.hue}, 100%, 50%)`;
                ctx.arc(fw.x, fw.y, 3, 0, Math.PI * 2);
                ctx.fill();
                
                // Tegn hale
                ctx.beginPath();
                ctx.strokeStyle = `hsla(${fw.hue}, 100%, 50%, 0.5)`;
                ctx.lineWidth = 2;
                ctx.moveTo(fw.x, fw.y);
                ctx.lineTo(fw.x, fw.y + 10);
                ctx.stroke();
            }
            
            // Tegn partikler
            for (const p of fw.particles) {
                ctx.globalAlpha = p.life;
                ctx.beginPath();
                ctx.fillStyle = p.color;
                ctx.arc(p.x, p.y, 3, 0, Math.PI * 2);
                ctx.fill();
            }
            ctx.globalAlpha = 1;
        }
    }
    
    // Start tidtakeren
    function startTimer() {
        if (!gameRunning) {
            gameRunning = true;
            startTime = Date.now();
            timerInterval = setInterval(updateTimer, 10); // Oppdater hvert 10ms for hundredeler
        }
    }
    
    // Stopp tidtakeren
    function stopTimer() {
        if (gameRunning) {
            gameRunning = false;
            clearInterval(timerInterval);
        }
    }
    
    // Oppdater tidtakeren
    function updateTimer() {
        const elapsedTime = (Date.now() - startTime) / 1000;
        
        // Formater tid som MM:SS.HH
        const minutes = Math.floor(elapsedTime / 60).toString().padStart(2, '0');
        const seconds = Math.floor(elapsedTime % 60).toString().padStart(2, '0');
        const hundredths = Math.floor((elapsedTime % 1) * 100).toString().padStart(2, '0');
        
        const timeString = `${minutes}:${seconds}.${hundredths}`;
        updateDigitDisplay(timeString);
    }
    
    // Reset spillet
    function resetGame() {
        // Sett vanskelighetsgrad og tilpass størrelsen
        setDifficulty(difficultySelect.value);
        
        // Generer ny labyrint
        generateMaze();
        
        // Reset meldinger
        messageElement.textContent = '';
        
        // Reset timer
        stopTimer();
        updateDigitDisplay('00:00.00');
        
        // Stopp fyrverkeri
        if (fireworkInterval) {
            clearInterval(fireworkInterval);
        }
        fireworks = [];
        gameCompleted = false;
        gameGivenUp = false;
        shortestPath = [];
        
        // Avbryt eventuelle pågående animasjonsrammer
        if (animationFrameId) {
            cancelAnimationFrame(animationFrameId);
            animationFrameId = null;
        }
        
        // Start spillet på nytt
        update();
    }
    
    // Gi opp spillet og vis korteste vei
    function giveUp() {
        if (!gameCompleted && !gameGivenUp) {
            gameGivenUp = true;
            stopTimer();
            messageElement.textContent = 'Du ga opp. Her er korteste vei ut!';
            
            // Finn korteste vei ut
            findShortestPath();
            
            // Oppdater visningen
            update();
        }
    }
    
    // Håndter tastetrykk
    document.addEventListener('keydown', (e) => {
        // Ikke gjør noe hvis spillet er fullført eller gitt opp
        if (gameCompleted || gameGivenUp) {
            e.preventDefault();
            return;
        }
        
        // Start tidtakeren ved første tastetrykk
        if (!gameRunning) {
            startTimer();
        }
        
        let newX = playerX;
        let newY = playerY;
        
        switch (e.key) {
            case 'ArrowUp':
                newY--;
                break;
            case 'ArrowDown':
                newY++;
                break;
            case 'ArrowLeft':
                newX--;
                break;
            case 'ArrowRight':
                newX++;
                break;
            default:
                return; // Ignorer andre taster
        }
        
        // Sjekk om den nye posisjonen er gyldig (ikke vegg)
        if (newY >= 0 && newY < maze.length && newX >= 0 && newX < maze[0].length && maze[newY][newX] !== 0) {
            playerX = newX;
            playerY = newY;
            update();
        }
        
        // Hindre at piltastene ruller siden
        e.preventDefault();
    });
    
    // Finn korteste vei fra spillerens posisjon til utgangen
    function findShortestPath() {
        // Bruk breadth-first search (BFS) for å finne korteste vei
        const queue = [{x: playerX, y: playerY, path: []}];
        const visited = Array(MAZE_HEIGHT).fill().map(() => Array(MAZE_WIDTH).fill(false));
        visited[playerY][playerX] = true;
        
        // Retninger: opp, høyre, ned, venstre
        const directions = [
            {dx: 0, dy: -1},
            {dx: 1, dy: 0},
            {dx: 0, dy: 1},
            {dx: -1, dy: 0}
        ];
        
        while (queue.length > 0) {
            const current = queue.shift();
            
            // Sjekk om vi har nådd målet
            if (current.x === exitX && current.y === exitY) {
                // Legg til målet i stien
                current.path.push({x: exitX, y: exitY});
                shortestPath = current.path;
                return;
            }
            
            // Prøv alle fire retninger
            for (const dir of directions) {
                const nx = current.x + dir.dx;
                const ny = current.y + dir.dy;
                
                // Sjekk om den nye posisjonen er gyldig og ikke er besøkt
                if (nx >= 0 && nx < MAZE_WIDTH && ny >= 0 && ny < MAZE_HEIGHT && 
                    maze[ny][nx] !== WALL && !visited[ny][nx]) {
                    
                    // Merk som besøkt
                    visited[ny][nx] = true;
                    
                    // Lag en kopi av stien og legg til den nye posisjonen
                    const newPath = [...current.path, {x: current.x, y: current.y}];
                    
                    // Legg til i køen
                    queue.push({x: nx, y: ny, path: newPath});
                }
            }
        }
        
        // Hvis vi kommer hit, fant vi ingen vei til målet
        return [];
    }
    
    // Legg til event listeners
    resetButton.addEventListener('click', resetGame);
    giveUpButton.addEventListener('click', giveUp);
    difficultySelect.addEventListener('change', resetGame);
    
    // Funksjon for å håndtere vindusendringer
    function handleResize() {
        // Vent litt for å unngå for mange oppdateringer
        clearTimeout(window.resizeTimer);
        window.resizeTimer = setTimeout(() => {
            // Beregn tilgjengelig plass basert på vindusstørrelse
            adjustCanvasSize();
            
            // Oppdater visningen uten å regenerere labyrinten
            update();
        }, 250);
    }
    
    // Funksjon for å justere canvas-størrelsen basert på vindusstørrelse
    function adjustCanvasSize() {
        // Beregn tilgjengelig plass
        const windowWidth = window.innerWidth;
        const windowHeight = window.innerHeight;
        
        // Beregn maksimal størrelse for spillområdet (ta hensyn til sidebar og marger)
        const sidebarWidth = 320; // Sidebar + litt margin
        const headerHeight = 60;  // Overskrift + litt margin
        const padding = 40;       // Padding rundt hele siden
        
        const maxGameWidth = windowWidth - sidebarWidth - padding;
        const maxGameHeight = windowHeight - headerHeight - padding;
        
        // Bruk den minste dimensjonen for å sikre et kvadratisk spillbrett
        const gameSize = Math.min(maxGameWidth, maxGameHeight);
        
        // Oppdater container-størrelsen
        const gameContainer = document.querySelector('.game-container');
        gameContainer.style.width = `${gameSize}px`;
        gameContainer.style.height = `${gameSize}px`;
        
        // Beregn nøyaktig cellestørrelse for å unngå padding
        cellSize = Math.floor(gameSize / MAZE_WIDTH);
        
        // Oppdater canvas størrelse (kvadratisk)
        canvas.width = canvas.height = cellSize * MAZE_WIDTH;
        
        console.log(`Window: ${windowWidth}x${windowHeight}, Game: ${gameSize}x${gameSize}, Canvas: ${canvas.width}x${canvas.height}, Cell: ${cellSize}`);
    }
    
    // Opprett timer display
    createTimerDisplay();
    
    // Sett standard vanskelighetsgrad
    setDifficulty('easy');
    
    // Generer første labyrint
    generateMaze();
    
    // Juster canvas-størrelsen ved oppstart
    adjustCanvasSize();
    
    // Start spillet
    update();
    
    // Legg til event listener for vindusendringer
    window.addEventListener('resize', handleResize);
});
