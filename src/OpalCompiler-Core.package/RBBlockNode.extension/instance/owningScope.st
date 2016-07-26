*opalcompiler-core
owningScope

	^ self scope ifNil: ["inlined" ^ parent owningScope]