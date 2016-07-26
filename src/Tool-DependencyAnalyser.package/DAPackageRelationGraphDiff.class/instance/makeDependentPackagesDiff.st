searching
makeDependentPackagesDiff
	"we're just looking at the package undeterminate"
	"every dependent packages are removed or new in older or new packages"
	self packagesDiffUnderteminate do: [ :item |
			self makeDependentPackagesDiffOn: item content.
		 ]
	