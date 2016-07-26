accessing
messageText
	^ 'Slot ''', name asString, ''' not found', (targetClass ifNil: [''] ifNotNil:[' in ', targetClass name])