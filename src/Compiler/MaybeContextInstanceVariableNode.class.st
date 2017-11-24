"
This class conspires to arrange that inst var access for contexts is done exclusively using the long-form instance variabl;e access bytecodes.  See InstructionStream class>>variablesAndOffsetsDo:.

A virtual machine can benefit in performance by organizing method and block activations using a  more conventional stack organization than by using first-class activation records (contexts).  But such a virtual machine is also cabable of hiding the stack and making it appear as if contexts are still used.  This means the system has better performance but still has all the benefits of first-class activation records.  To pull this off the VM needs to intercept any and all accesses to context objects so that it can make contexts function as proxy objects for stack frames.

Without help from the image such a virtual machine based on an interpreter would have to perform an expensive check on all instance variable accesses to determine if the instance variable was that of a context serving as a proxy for a stack frame.  A simple hack is to take advantage of the short and long forms of instance variable access bytecodes.  The BlueBook instruction set (and likely any bytecode set evolved from it) has short form bytecodes for fetching and storing the first few bytecodes (BlueBook fetch first 16, store first 8).  Contexts typically have at most 6 instance variables.  If we arrange to use the long-form bytecodes for all context inst var accesses then we only have to check for context inst var access in long-form bytecodes, and then only if the index is within the context inst var range.  This effectively makes the check free because on modern processors checking an index fetched from memory into a register against a constant costs far less than the memry read to fetch the index.
"
Class {
	#name : #MaybeContextInstanceVariableNode,
	#superclass : #InstanceVariableNode,
	#category : #'Compiler-ParseNodes'
}

{ #category : #accessing }
MaybeContextInstanceVariableNode >> code [
	"Answer a bogus code to avoid creating quick methods.
	 See MethodNode>>generate:ifQuick:"
	^LoadLong
]

{ #category : #'code generation' }
MaybeContextInstanceVariableNode >> emitCodeForStore: stack encoder: encoder [
	encoder genStoreInstVarLong: index
]

{ #category : #'code generation' }
MaybeContextInstanceVariableNode >> emitCodeForStorePop: stack encoder: encoder [
	encoder genStorePopInstVarLong: index.
	stack pop: 1
]

{ #category : #'code generation' }
MaybeContextInstanceVariableNode >> emitCodeForValue: stack encoder: encoder [
	stack push: 1.
	^encoder genPushInstVarLong: index
]

{ #category : #'code generation' }
MaybeContextInstanceVariableNode >> sizeCodeForStore: encoder [
	^encoder sizeStoreInstVarLong: index
]

{ #category : #'code generation' }
MaybeContextInstanceVariableNode >> sizeCodeForStorePop: encoder [
	^encoder sizeStorePopInstVarLong: index
]

{ #category : #'code generation' }
MaybeContextInstanceVariableNode >> sizeCodeForValue: encoder [
	^encoder sizePushInstVarLong: index
]
