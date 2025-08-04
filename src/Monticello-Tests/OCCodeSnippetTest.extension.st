Extension { #name : 'OCCodeSnippetTest' }

{ #category : '*Monticello-Tests' }
OCCodeSnippetTest >> testMonticello [

	| definition |

	"Force non method to be inside a method"
	snippet isScripting ifTrue: [
		snippet := snippet copy.
		snippet source: 'foo ' , snippet source ].

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

	snippet isFaultyMinusUndeclared ifTrue: [
			self should: [ 	[ definition load ] on: Warning do: [ :e | e resume ]. "Ignore Selector missmatches" ] raise: OCCodeError.
			self assert: MCMockClassE methods size equals: 0.
			^ self ].

	[ definition load ] on: Warning do: [ :e | e resume ]. "Ignore Selector missmatches"
	self assert: MCMockClassE methods size equals: 1.

	"forced methods cannot be executed, because no return (pharo language is weird)"
	snippet isScripting ifFalse: [ self testExecute: MCMockClassE >> definition selector ].

	definition unload.
	self assert: MCMockClassE methods size equals: 0
]
