private
entryByLocalName

	^ entryByLocalName ifNil: [ entryByLocalName := WeakValueDictionary new ]