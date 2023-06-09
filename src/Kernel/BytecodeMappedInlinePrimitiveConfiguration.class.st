"
My subclasses configure `InstructionClient`s to handle bytecode extensions.

They send clients the `addMappedInlinePrimitiveHandler:at:` message to add bytecode handlers.
"
Class {
	#name : #BytecodeMappedInlinePrimitiveConfiguration,
	#superclass : #Object,
	#category : #'Kernel-Methods'
}

{ #category : #configuring }
BytecodeMappedInlinePrimitiveConfiguration class >> configure: instructionClient [
	"Send the `addMappedInlinePrimitiveHandler:at:` message to the instructionClient in order to add bytecode handlers"

	self subclassResponsibility
]
