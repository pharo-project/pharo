private
iceCommitFrom: aCommitish
	"The type of 'aCommitish' depends on how each backend internal representation. "
	^ self frontend
		commitAt: (self commitIdFrom: aCommitish )
		ifAbsentPut: [ self class parseCommitInfo: aCommitish ]