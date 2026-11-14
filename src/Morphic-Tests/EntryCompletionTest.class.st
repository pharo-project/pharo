"
I test `EntryCompletion`, the completion popup of a `RubTextFieldMorph`.

## Responsibility:

- Check how keys typed in the field reach the chooser of choices.

## Collaborators:

- `EntryCompletion`: the object under test.
- `RubTextFieldMorph`: the field that forwards its keystrokes to the completion.
- `IdentifierChooserMorph`: the popup listing the choices.

## Public API and Key Messages

- `testArrowDownInTheFieldSelectsTheFirstChoice`

```
EntryCompletionTest run: #testArrowDownInTheFieldSelectsTheFirstChoice
```
"
Class {
	#name : 'EntryCompletionTest',
	#superclass : 'TestCase',
	#instVars : [
		'field',
		'completion'
	],
	#category : 'Morphic-Tests',
	#package : 'Morphic-Tests'
}

{ #category : 'helpers' }
EntryCompletionTest >> arrowDownEvent [

	^ KeyboardEvent new
		  setType: #keystroke
		  buttons: 0
		  position: 0 @ 0
		  keyValue: Character arrowDown asInteger
		  charCode: Character arrowDown asInteger
		  hand: self currentWorld activeHand
		  stamp: 0
]

{ #category : 'helpers' }
EntryCompletionTest >> selectedChoice [

	^ completion chooser choiceMenus first selectedItem ifNotNil: [ :item | item contents asString ]
]

{ #category : 'running' }
EntryCompletionTest >> setUp [
	super setUp.
	completion := EntryCompletion new
		              dataSourceBlock: [ :text | #( 'pharo' 'spec' ) ];
		              filterBlock: [ :each :text | true ];
		              yourself.
	field := RubTextFieldMorph new.
	field entryCompletion: completion.
	field setText: 'p'
]

{ #category : 'running' }
EntryCompletionTest >> tearDown [
	completion closeChooser.
	super tearDown
]

{ #category : 'tests' }
EntryCompletionTest >> testArrowDownInTheFieldSelectsTheFirstChoice [
	"While the chooser is open, the down arrow typed in the field moves into the list of choices"

	field openChooser.
	completion keystroke: self arrowDownEvent from: field.
	self assert: self selectedChoice equals: 'pharo'
]
