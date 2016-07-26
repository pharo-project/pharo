tests - testing
testIsInstalled
|  method cls |

	method := (self class)>>#returnTrue.
	self assert: method isInstalled.

	"now make an orphaned method by just deleting the class."

	Smalltalk removeClassNamed: #TUTU.

	cls := Object subclass: #TUTU
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self categoryNameForTemporaryClasses.
	cls compile: 'foo ^ 10'.
	method := cls >> #foo.
	Smalltalk removeClassNamed: #TUTU.

	self deny: method isInstalled. 