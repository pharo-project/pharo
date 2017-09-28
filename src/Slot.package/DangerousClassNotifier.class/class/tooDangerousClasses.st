accessing
tooDangerousClasses
	"Return a list of class names which will not be modified in the public interface"
	^#(
		"Object will break immediately"
		#ProtoObject #Object
		"Contexts and their superclasses"
		#InstructionStream #Context #BlockClosure #CompiledMethod #CompiledCode #CompiledBlock
	)