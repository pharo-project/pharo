geometry
vExtraScrollRange
	"Return the amount of extra blank space to include below the bottom of the scroll content."
	"The classic behavior would be ^bounds height - (bounds height * 3 // 4)"
	"Takes into accound the top header height if present"
	^ super vExtraScrollRange + self topHeaderHeight 
