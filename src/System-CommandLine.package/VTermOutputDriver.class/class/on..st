instance-creation
on: anOutputStream
	^ self new
		outStream: (ZnCharacterWriteStream on: anOutputStream encoding: 'utf8');
		yourself