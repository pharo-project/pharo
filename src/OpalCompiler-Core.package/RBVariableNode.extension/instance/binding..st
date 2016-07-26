*opalcompiler-core
binding: aSemVar
	aSemVar ifNil: [^self removeProperty: #binding ifAbsent: []].
	self propertyAt: #binding put: aSemVar.