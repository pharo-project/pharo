tabs
calculateVisibleTabs
	"Answer the tabs that should be visible given the space."

	|widths answer availableWidth accumulatedWidth tabStr widthStr width|
	widths := self tabs collect: [:t | t minExtent x].
	availableWidth := self layoutBounds width.
	self cornerStyle = #rounded
		ifTrue: [availableWidth := availableWidth - 6].
	widths sum <= availableWidth ifTrue: [^self tabs].
	availableWidth := availableWidth - self controlsExtent x.
	answer := OrderedCollection new.
	accumulatedWidth := 0.
	self visibleTabRange last = 0
		ifTrue: [tabStr := (self tabs copyFrom: self visibleTabRange first to: self tabs size) readStream.
				widthStr := (widths copyFrom: self visibleTabRange first to: widths size) readStream.
				[accumulatedWidth <= availableWidth and: [tabStr atEnd not]] whileTrue: [
					answer add: tabStr next.
					accumulatedWidth := accumulatedWidth + widthStr next].
				accumulatedWidth > availableWidth ifTrue: [
					answer removeLast].
				^answer]
		ifFalse: [tabStr := (self tabs copyFrom: 1 to: self visibleTabRange last) reversed readStream.
				widthStr := (widths copyFrom: 1 to: self visibleTabRange last) reversed readStream.
				[accumulatedWidth <= availableWidth and: [tabStr atEnd not]] whileTrue: [
					answer addFirst: tabStr next.
					accumulatedWidth := accumulatedWidth + (width := widthStr next)].
				accumulatedWidth > availableWidth ifTrue: [
					answer removeFirst.
					accumulatedWidth := accumulatedWidth - width].
				tabStr := (self tabs copyFrom: self visibleTabRange last + 1 to: self tabs size) readStream.
				widthStr := (widths copyFrom: self visibleTabRange last + 1 to: widths size) readStream.
				[accumulatedWidth <= availableWidth and: [tabStr atEnd not]] whileTrue: [
					answer add: tabStr next.
					accumulatedWidth := accumulatedWidth + widthStr next].
				accumulatedWidth > availableWidth
					ifTrue: [answer removeLast].
				^answer]
	
		
	