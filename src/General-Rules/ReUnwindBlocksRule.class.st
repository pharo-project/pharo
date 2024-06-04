"
Move assignment out of unwind blocks.

For example

[[[
[ statements. 
var := object ] ifCurtailed: block
]]]


[[[ 
var := [ statements. 
	    object] ifCurtailed: block
]]]
"
Class {
	#name : 'ReUnwindBlocksRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReUnwindBlocksRule class >> group [
	^ 'Optimization'
]

{ #category : 'accessing' }
ReUnwindBlocksRule class >> ruleName [
	^ 'Move assignment out of unwind blocks'
]

{ #category : 'accessing' }
ReUnwindBlocksRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'UnwindBlocksRule'
]

{ #category : 'initialization' }
ReUnwindBlocksRule >> initialize [
	super initialize.
	self
		replace: '[| `@temps | ``@.Statements. `var := ``@object] ensure: ``@block'
			with: '`var := [| `@temps | ``@.Statements. ``@object] ensure: ``@block';
		replace: '[| `@temps | ``@.Statements. ^``@object] ensure: ``@block'
			with: '^[| `@temps | ``@.Statements. ``@object] ensure: ``@block';
		replace:'[| `@temps | ``@.Statements. `var := ``@object] ifCurtailed: ``@block'
			with: '`var := [| `@temps | ``@.Statements. ``@object] ifCurtailed: ``@block';
		replace:'[| `@temps | ``@.Statements. ^``@object] ifCurtailed: ``@block'
			with: '^[| `@temps | ``@.Statements. ``@object] ifCurtailed: ``@block'
]
