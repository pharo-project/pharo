tests - accessing
testMethodClass
	| method cls |
	method := self class >> #returnTrue.
	self assert: method selector = #returnTrue.
	"now make an orphaned method by just deleting the class.
	old: #unknown
	new semantics: return Absolete class"
	Smalltalk removeClassNamed: #TUTU.
	cls := Object
				subclass: #TUTU
				instanceVariableNames: ''
				classVariableNames: ''
				poolDictionaries: ''
				category: self categoryNameForTemporaryClasses.
	cls compile: 'foo ^ 10'.
	method := cls >> #foo.
	Smalltalk removeClassNamed: #TUTU.
	self assert: method methodClass = cls