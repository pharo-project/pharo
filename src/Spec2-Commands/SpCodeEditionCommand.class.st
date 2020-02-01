"
I'm a base command for edition in code presenters. 
Operations like ""copy"", ""paste"" will be implemented by extending me. 
"
Class {
	#name : #SpCodeEditionCommand,
	#superclass : #SpToolCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #accessing }
SpCodeEditionCommand >> editor [
	self flag: 'This is wrong! How can we edit text/code from text/code presenters?'.
	self flag: 'Class name is wrong: it also applies to text and not only to code!'.
	^ context adapter widget textArea editor
]
