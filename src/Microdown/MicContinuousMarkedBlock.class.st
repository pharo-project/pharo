"
I'm an abstract block for blocks requiring a systematic simple character or character sequences at the beginning of each line. The text of the lines are accumulated for now as line without the markup separated by a newline. 

For example 
```
% comment 1
% comment 2
```
produces a single comment object with text as follows:

```
comment 1
comment 2
```


For example 
```
> this is an example
> and another one
```

produces 

> this is an example
> and another one




"
Class {
	#name : #MicContinuousMarkedBlock,
	#superclass : #MicAbstractBlock,
	#instVars : [
		'text'
	],
	#category : #'Microdown-Model'
}

{ #category : #actions }
MicContinuousMarkedBlock >> addLineAndReturnNextNode: line [
	"line is assumed to be of the form 'chararacters some text' e.g., '> some text'
	the prefix spaces after $> are removed"

	| treatedLine |
	treatedLine := self extractLine: line.
	text := text
		ifNil: [ treatedLine ]
		ifNotNil: [ text , String cr , treatedLine ]
]

{ #category : #testing }
MicContinuousMarkedBlock >> canConsumeLine: line [
	"return if this block can consume line i.e., it starts with sensible characters, e.g., > "

	^ self doesLineStartWithMarkup: line
]

{ #category : #testing }
MicContinuousMarkedBlock >> doesLineStartWithMarkup: line [
	"return if the line starts with a markup"

	^ line beginsWith: self lineMarkup
	
]

{ #category : #actions }
MicContinuousMarkedBlock >> extractLine: line [
	^ (line copyFrom: self lineMarkup size + 1 to: line size) trim
]

{ #category : #markups }
MicContinuousMarkedBlock >> lineMarkup [

	^ self subclassResponsibility 
]

{ #category : #accessing }
MicContinuousMarkedBlock >> text [
	^ text
]
