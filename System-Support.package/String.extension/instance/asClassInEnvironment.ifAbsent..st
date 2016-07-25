*System-Support
asClassInEnvironment: anEnvironment ifAbsent: absentBlock
	^  anEnvironment 
			at: self asSymbol 
			ifPresent: [ :aClass| 
				self 
					assert: aClass isBehavior 
					description: [ self printString, ' is not a Class but an instance of a ', aClass class name ].
				aClass ]
			ifAbsent: absentBlock
	