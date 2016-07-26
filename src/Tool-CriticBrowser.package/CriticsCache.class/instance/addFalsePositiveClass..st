add/remove
addFalsePositiveClass: aClass 
	
	| fp |
	falsePositiveClasses add: aClass theNonMetaClass. 
	critics keysAndValuesDo: [ :rule :criticss |
		fp := criticss select: [ :critic | aClass = critic criticTheNonMetaclassClass ].
		fp do: [ :each | self addFalsePositive: each forRule: rule] ].
	self updateBrowser	
