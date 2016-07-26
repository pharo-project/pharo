testing
includesSelector: selector 
	^ protocols anySatisfy: [ :each | each includesSelector: selector ]