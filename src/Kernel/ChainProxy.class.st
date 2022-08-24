"
A proxy that allows for more functional style of programming. Any message send returns the result as a new receiver for the next message. This can avoid too many nested parentheses and result in more cleaner written code.

Example:

```language=Pharo
(((#(apple peach banana)  
	groupedBy: #size) select: [:each | each size even]) values) collect: #asCommaString
```
	
can be writte more clearly as:

```language=Pharo
 #(apple peach banana) chain
	groupedBy: #size;
	select: [:each | each size even];
	values;
	collect: #asCommaString
```	
"
Class {
	#name : #ChainProxy,
	#superclass : #Object,
	#instVars : [
		'target'
	],
	#category : #'Kernel-Utilities'
}

{ #category : #examples }
ChainProxy class >> example [
	<script: 'self example inspect'>
	
	^ #(apple peach banana) chain
			groupedBy: #size;
			select: [:each | each size even];
			values;
			collect: #asCommaString
]

{ #category : #'reflective operations' }
ChainProxy >> doesNotUnderstand: aMessage [

	target := aMessage sendTo: target.
	^ target
]

{ #category : #'private - accessing' }
ChainProxy >> setTarget: anObject [
	"We use unusal #setTarget: to allow #target: to get proxied"
	
	target := anObject.
	^ self
]
