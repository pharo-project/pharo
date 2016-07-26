accessing
monitorType
  "#LCD or #CRT"
  ^ self subPixelAntiAliasing ifTrue: [#LCD] ifFalse: [#CRT]
