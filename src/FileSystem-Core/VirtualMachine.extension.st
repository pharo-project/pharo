Extension { #name : #VirtualMachine }

{ #category : #'*FileSystem-Core' }
VirtualMachine >> binary [
	"Answer the a file reference to the virtual machine binary."
	"SmalltalkImage current vmBinary"

	^ FileLocator vmBinary

]
