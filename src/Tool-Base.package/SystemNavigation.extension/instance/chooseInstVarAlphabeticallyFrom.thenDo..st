*Tool-Base
chooseInstVarAlphabeticallyFrom: aClass thenDo: aBlock
	| allVars index |
	"Put up a menu of all the instance variables in the receiver, presented in alphabetical order, and when the user chooses one, evaluate aBlock with the chosen variable as its parameter."

	allVars := aClass allInstVarNames sort.
	allVars isEmpty ifTrue: [^ self inform: 'There are no instance variables'].

	index := (UIManager default chooseFrom: allVars lines: #() title: 'Instance variables in ', aClass name).
	index = 0 ifTrue: [^ aClass].
	aBlock value: (allVars at: index)