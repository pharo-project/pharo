utils
createClassUsing: aTrait
	^ Object
		subclass: #AClassForTests
		uses: aTrait
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'AAA'