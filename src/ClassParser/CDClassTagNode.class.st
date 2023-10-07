"
I'm a simple node to hold a string representing a tag. A tag is a optional string based part to be able to sort class in subpackage folders.

```
Object < #CDClassDefinitionParser
	slots: {#classDefinition . #subclass};
	tag: 'Model';
	package: 'ClassParser'
```

and in old syntax

```
Object subclass: #CDClassDefinitionParser
	instanceVariableNames: 'classDefinition subclass'
	classVariableNames: ''
	package: 'ClassParser-Model'
```
"
Class {
	#name : 'CDClassTagNode',
	#superclass : 'CDNode',
	#instVars : [
		'name'
	],
	#category : 'ClassParser-Model',
	#package : 'ClassParser',
	#tag : 'Model'
}

{ #category : 'accessing' }
CDClassTagNode >> name [
	^ name
]

{ #category : 'accessing' }
CDClassTagNode >> name: aString [
	name := aString
]
