convenience
mimeDecode: aStringOrStream as: contentsClass
	^ contentsClass streamContents: [:out |
		self mimeDecode: aStringOrStream to: out]