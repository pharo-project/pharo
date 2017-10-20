instance-creation
on: anOutputStream
	^ self new
		outStream: (ZnCrPortableWriteStream on:
			(ZnCharacterWriteStream on: anOutputStream encoding: 'utf8'));
		yourself