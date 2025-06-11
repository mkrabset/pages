class JasyptPBEWITHHMACSHA512ANDAES256 {
  constructor(options = {}) {
    // Standardverdier
    this.iterationCount = options.iterationCount || 1000;
    this.keySize = 32; // AES-256 bruker 32 bytes (256 bits) nøkkel
    this.ivSize = 16;  // AES bruker 16 bytes (128 bits) IV
    this.saltSize = options.saltSize || 16;
  }

  /**
   * Genererer et tilfeldig salt
   * @returns {Uint8Array} Salt som byte array
   */
  async generateSalt() {
    return crypto.getRandomValues(new Uint8Array(this.saltSize));
  }

  /**
   * Genererer en tilfeldig initialiseringsvektor (IV)
   * @returns {Uint8Array} IV som byte array
   */
  async generateIV() {
    return crypto.getRandomValues(new Uint8Array(this.ivSize));
  }

  /**
   * Avleder en nøkkel fra passord og salt ved hjelp av PBKDF2 med HMAC-SHA512
   * @param {string} password - Passordet som brukes for kryptering
   * @param {Uint8Array} salt - Salt som brukes i nøkkelavledningen
   * @returns {CryptoKey} Den avledede nøkkelen
   */
  async deriveKey(password, salt) {
    // Konverter passord til ArrayBuffer
    const encoder = new TextEncoder();
    const passwordBuffer = encoder.encode(password);
    
    // Importer passordet som en rå nøkkel
    const passwordKey = await crypto.subtle.importKey(
      'raw',
      passwordBuffer,
      { name: 'PBKDF2' },
      false,
      ['deriveKey']
    );
    
    // Avled AES-nøkkel ved hjelp av PBKDF2 med HMAC-SHA512
    return crypto.subtle.deriveKey(
      {
        name: 'PBKDF2',
        salt: salt,
        iterations: this.iterationCount,
        hash: 'SHA-512'
      },
      passwordKey,
      {
        name: 'AES-CBC',
        length: this.keySize * 8 // Lengde i bits
      },
      false,
      ['encrypt', 'decrypt']
    );
  }

  /**
   * Krypterer en streng med det gitte passordet
   * @param {string} plaintext - Teksten som skal krypteres
   * @param {string} password - Passordet som brukes for kryptering
   * @returns {string} Base64-kodet kryptert tekst med salt og IV
   */
  async encrypt(plaintext, password) {
    // Generer salt og IV
    const salt = await this.generateSalt();
    const iv = await this.generateIV();
    
    // Avled nøkkel fra passord og salt
    const key = await this.deriveKey(password, salt);
    
    // Konverter klartekst til ArrayBuffer
    const encoder = new TextEncoder();
    const plaintextBuffer = encoder.encode(plaintext);
    
    // Krypter data
    const encryptedBuffer = await crypto.subtle.encrypt(
      {
        name: 'AES-CBC',
        iv: iv
      },
      key,
      plaintextBuffer
    );
    
    // Kombiner salt, IV og kryptert data
    const encryptedArray = new Uint8Array(encryptedBuffer);
    const result = new Uint8Array(salt.length + iv.length + encryptedArray.length);
    
    result.set(salt, 0);
    result.set(iv, salt.length);
    result.set(encryptedArray, salt.length + iv.length);
    
    // Konverter til Base64
    return this._arrayBufferToBase64(result);
  }

  /**
   * Dekrypterer en tidligere kryptert streng
   * @param {string} encryptedText - Base64-kodet kryptert tekst med salt og IV
   * @param {string} password - Passordet som brukes for dekryptering
   * @returns {string} Dekryptert tekst
   */
  async decrypt(encryptedText, password) {
    // Konverter Base64 til ArrayBuffer
    const encryptedData = this._base64ToArrayBuffer(encryptedText);
    
    // Hent salt, IV og kryptert data
    const salt = encryptedData.slice(0, this.saltSize);
    const iv = encryptedData.slice(this.saltSize, this.saltSize + this.ivSize);
    const ciphertext = encryptedData.slice(this.saltSize + this.ivSize);
    
    // Avled nøkkel fra passord og salt
    const key = await this.deriveKey(password, salt);
    
    // Dekrypter data
    const decryptedBuffer = await crypto.subtle.decrypt(
      {
        name: 'AES-CBC',
        iv: iv
      },
      key,
      ciphertext
    );
    
    // Konverter dekryptert data til streng
    const decoder = new TextDecoder();
    return decoder.decode(decryptedBuffer);
  }

  /**
   * Konverterer ArrayBuffer til Base64-streng
   * @param {Uint8Array} buffer - Data som skal konverteres
   * @returns {string} Base64-kodet streng
   */
  _arrayBufferToBase64(buffer) {
    let binary = '';
    const bytes = new Uint8Array(buffer);
    for (let i = 0; i < bytes.byteLength; i++) {
      binary += String.fromCharCode(bytes[i]);
    }
    return btoa(binary);
  }

  /**
   * Konverterer Base64-streng til ArrayBuffer
   * @param {string} base64 - Base64-kodet streng
   * @returns {Uint8Array} Dekodede data
   */
  _base64ToArrayBuffer(base64) {
    const binaryString = atob(base64);
    const bytes = new Uint8Array(binaryString.length);
    for (let i = 0; i < binaryString.length; i++) {
      bytes[i] = binaryString.charCodeAt(i);
    }
    return bytes;
  }
}

// Eksporter klassen for bruk i andre moduler
if (typeof module !== 'undefined' && module.exports) {
  module.exports = JasyptPBEWITHHMACSHA512ANDAES256;
}
