# Cryptomeria
Cryptomeria is a thin wrapper library around TPLockBox, which enables you to encrypt/decrypt data easily and safely.

### Usage

```delphi
    //...
    FCryptorRSA: ICryptorRSA;
    FCryptorAES: ICryptorAES;
    FKeyPairGen: IKeyPairGenerator;
	
	// ...Create instances...
	
	FCryptorRSA.EncryptFile(PlantextFile, CiphertextFile, KeyFile);
	FCryptorRSA.DecryptFile(PlantextFile, CiphertextFile, KeyFile);
	
	FCryptorAES.EncryptString(Plaintext, Ciphertext, Password);
	FCryptorAES.DecryptString(Plaintext, Ciphertext, Password);	
```

### Depedencies

- TurboPower LockBox 3.6.3 (http://lockbox.seanbdurkin.id.au/HomePage / https://github.com/SeanBDurkin/tplockbox)

### Compilation guide

In order to compile this sources on Windows, you need to install the Embarcadero RAD Studio 10 Seattle environment.

### License

This software is released under the GPLv3 license. See LICENSE.md.

# CryptomeriaGUI
Proof-of-concept GUI application based on Cryptomeria.

### Depedencies

- TurboPower LockBox 3.6.3 (http://lockbox.seanbdurkin.id.au/HomePage / https://github.com/SeanBDurkin/tplockbox)
- Cryptomeria
- Spring4D 1.1.2 (https://bitbucket.org/sglienke/spring4d)

### Screenshot
![Screenshot](https://habrastorage.org/files/d8d/e02/57b/d8de0257b494468a893af17675b4df50.png)

### License

This software is released under the GPLv3 license. See LICENSE.md.