invariant
invariant
	"If any of these exceptions rise, please notify us through moose-dev mail list. Your help is appreciated"
	
	self isDebug ifFalse: [ ^ self ].
	
	pagesCount >= 0 ifFalse: [ GLMPagerException signal: 'The number of page must be non negative' ].

	pagesCount > 1 ifTrue: [
		firstVisiblePageIndex >= 1 ifFalse: [GLMPagerException signal: 'First visible page index must be > 1 if number of pages > 0'].
		lastVisiblePageIndex >= 1 ifFalse: [GLMPagerException signal: 'Last visible page index must be > 1 if number of pages > 0'].
		lastVisiblePageIndex <= pagesCount ifFalse: [ GLMPagerException signal: 'Last visible page index must be less or equals to the number of pages' ].
		self actualNumberOfVisiblePages <= numberOfVisiblePages ifFalse: [ GLMPagerException signal: 'Actual number of visible pages must be less or equals to current number of visible pages' ].
		selectedPageIndex >= 1 ifFalse: [ GLMPagerException signal: 'Selected page index must be greater or equals then 1 if number of pages is more than 1' ].
	].

	pagesCount = 1 ifTrue: [ 
		firstVisiblePageIndex = 1 ifFalse: [GLMPagerException signal: 'First visible page index must be 1 if number of pages is 1'].
		lastVisiblePageIndex = 1 ifFalse: [GLMPagerException signal: 'Last visible page index must be 1 if number of pages is 1'].
		lastVisiblePageIndex = firstVisiblePageIndex ifFalse: [ GLMPagerException signal: 'First and Last visible page indexes must be equal if number of pages is 1' ].
		self actualNumberOfVisiblePages = 1 ifFalse: [ GLMPagerException signal: 'Actual number of visible pages must be 1 if number of pages is 1' ].
		selectedPageIndex = 1 ifFalse: [ GLMPagerException signal: 'Selected page index must be 1 if number of pages is 1' ].
	].

	pagesCount = 0 ifTrue: [ 
		firstVisiblePageIndex = 0 ifFalse: [GLMPagerException signal: 'First visible page index must be 0 if number of pages is 0'].
		lastVisiblePageIndex = 0 ifFalse: [GLMPagerException signal: 'Last visible page index must be 0 if number of pages is 0'].
		self actualNumberOfVisiblePages = 0 ifFalse: [ GLMPagerException signal: 'Actual number of visible pages must be 0 if there is no pages' ].
		selectedPageIndex = 0 ifFalse: [ GLMPagerException signal: 'Selected page index must be 0 if number of pages is 0' ].
	]