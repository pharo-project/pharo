Extension { #name : #RBCodeSnippetTest }

{ #category : #'*Monticello-Tests' }
RBCodeSnippetTest >> testMonticello [

	| extensionPackage definition |

	"Force non method to inside a method"
	snippet isMethod ifFalse: [ snippet := snippet copy. snippet source: 'foo ' , snippet source ].

	extensionPackage := (MCWorkingCopy forPackage: (MCPackage named: 'FooBarBaz')).
	definition := MCMethodDefinition
		className: #MCMockClassE
		classIsMeta: false
		selector: #'' "do not care, load will repear it"
		category: nil
		timeStamp: nil
		source: snippet source.

	"MCMockClassE should have no method. cleanup if some previous failed tests invalidated that"
	MCMockClassE methods copy do: [ :m | MCMockClassE removeSelector: m selector ].
	self assert: MCMockClassE methods size equals: 0.

	snippet isFaulty ifTrue: [
			self should: [ definition load ] raise: CodeError.
			self assert: MCMockClassE methods size equals: 0.
			^ self ].

	[ definition load ] on: Warning do: [ :e | e resume ]. "Ignore Selector missmatches"
	self assert: MCMockClassE methods size equals: 1.

	"forced methods cannot be executed, because no syntetic return (pharo language is weird)"
	snippet isMethod ifTrue: [ self testExecute: MCMockClassE >> definition selector ].

	definition unload.
	self assert: MCMockClassE methods size equals: 0
]
