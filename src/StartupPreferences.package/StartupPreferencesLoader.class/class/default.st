instance creation
default

	^ UniqueInstance ifNil: [ UniqueInstance := super basicNew initialize ]