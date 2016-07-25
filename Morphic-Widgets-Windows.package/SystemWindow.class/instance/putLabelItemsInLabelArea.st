layout
putLabelItemsInLabelArea
	"Put label items into the label area, if there is one"

	labelArea ifNotNil:
		[stripes ifNotNil: [stripes do: [:stripe | labelArea addMorph: stripe]].
		closeBox ifNotNil: [labelArea addMorph: closeBox].
		menuBox ifNotNil: [labelArea addMorph: menuBox].
		collapseBox ifNotNil: [labelArea addMorph: collapseBox].
		label ifNotNil: [labelArea addMorph: label]]

