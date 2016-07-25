submorphs-add/remove
submorphIndexOf: aMorph
	"Assuming aMorph to be one of my submorphs, answer where it occurs in my submorph list"

	^ submorphs indexOf: aMorph ifAbsent: [nil]