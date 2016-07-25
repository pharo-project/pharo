initialization
initialize
	"MenuItemMorph initialize"

	| f |
	f := Form
		extent: 5@9
		fromArray: #(2147483648 3221225472 3758096384 4026531840 4160749568 4026531840 3758096384 3221225472 2147483648)
		offset: 0@0.
	SubMenuMarker := ColorForm mappingWhiteToTransparentFrom: f.
