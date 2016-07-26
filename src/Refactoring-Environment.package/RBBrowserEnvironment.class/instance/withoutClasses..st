environments
withoutClasses: aListOfClasses
	"Build the same environment without classes given as parameter"
	^ RBClassEnvironment 
			onEnvironment: self 
			classes: (self classes copyWithoutAll: aListOfClasses)