accessing
allResolvedIn: aRepository
	"Answer all sorted references within aRepository. For efficiency cache the references."

	^ (resolvedReferencesCache ifNil: [ resolvedReferencesCache := Dictionary new ])
		at: aRepository ifAbsentPut: [ self basicReferencesIn: aRepository ]