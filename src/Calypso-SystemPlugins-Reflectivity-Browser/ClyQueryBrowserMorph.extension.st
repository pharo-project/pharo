Extension { #name : 'ClyQueryBrowserMorph' }

{ #category : '*Calypso-SystemPlugins-Reflectivity-Browser' }
ClyQueryBrowserMorph class >> browseBreakpoints [
	"Excludes methods that contain halts marked as for testing. To browse all breakpoints, see this class's browseBreakpointsIncludingHaltsForTesting method"
	| query |
	query := ClyActiveBreakpointsQuery as: (ClySortedQueryResult using: ClySortMethodByPackageFunction ascending).
	^ClyQueryBrowserMorph openOn: query
]

{ #category : '*Calypso-SystemPlugins-Reflectivity-Browser' }
ClyQueryBrowserMorph class >> browseBreakpointsIncludingHaltsForTesting [
	| query |
	query := ClyActiveBreakpointsQuery newIncludingHaltsForTesting withResult: (ClySortedQueryResult using: ClySortMethodByPackageFunction ascending).
	^ClyQueryBrowserMorph openOn: query
]
