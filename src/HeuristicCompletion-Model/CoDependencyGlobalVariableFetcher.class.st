"
`CoDependencyPackageScopedGlobalVariableFetcher` is a global variable fetcher that provides code completion suggestions based on the **direct dependencies** of the current package. It scopes the completion results to what the current class or package is legally allowed to see, helping enforce **modular visibility** and **project-level encapsulation**.

### Core Responsibilities

- **Respects Direct Dependencies**: It fetches and caches the list of directly dependent packages for a given package using `StDependencyChecker`.
- **Scoped Suggestions**: Completion suggestions include:
  1. Classes defined in the current package,
  2. Classes from directly dependent packages,
  3. Remaining global variables from the environment (excluding duplicates).
- **Caching for Efficiency**: Dependencies are memoized per package to avoid recomputation.

### How It Works

1. **Dependency Resolution**:
   - Uses a class-side cache (`dependencies`) to store the direct dependencies of each package.
   - Fetches dependencies on-demand via `computeDependenciesOf:` if not already cached.

2. **Completion Logic in `entriesDo:`**:
   - If `completionClass` is not set, it defaults to fetching all globals from the environment.
   - Otherwise:
     - Adds classes defined in the current package.
     - Adds classes from directly dependent packages.
     - Avoids duplicates by removing already-added names from the global environment list.
     - Finally, adds global environment entries that match the filter and weren’t already included.

3. **Package Organizer Awareness**:
   - Uses the `PackageOrganizer` to resolve actual package instances by name and retrieve their defined classes.

### Use Case

Use this fetcher when:
- You want completions to be **strictly scoped to direct dependencies**, such as in highly modular systems or layered architectures.
- You're building tooling that mimics real code accessibility—helping developers **stay within their allowed boundaries**.
- You care about **completion relevance** in the context of modular design.

### Notes

- Unlike transitive fetchers, this one **does not recurse** through dependency trees—it sticks to direct dependencies only.
- It also avoids mutating or over-filtering the global environment, honoring a clear separation of fetcher concerns vs environment concerns.

"
Class {
	#name : 'CoDependencyGlobalVariableFetcher',
	#superclass : 'CoAbstractPackageDependencyFetcher',
	#instVars : [
		'dependencies'
	],
	#category : 'HeuristicCompletion-Model-Fetchers',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Fetchers'
}

{ #category : 'accessing' }
CoDependencyGlobalVariableFetcher >> dependencies [

	^ dependencies
]

{ #category : 'accessing' }
CoDependencyGlobalVariableFetcher >> dependencies: anObject [

	dependencies := anObject
]

{ #category : 'enumerating' }
CoDependencyGlobalVariableFetcher >> entriesDo: aBlock [

	| dependentClassNames dependentPackages |
	completionClass ifNil: [ ^ self ].

	self packageLocalClasses: aBlock.

	dependentClassNames := SortedCollection sortUsing: [ :a :b | a <= b ].
	dependentPackages := self dependenciesOf: package name.
	dependentPackages do: [ :packageName |
		dependentClassNames addAll: ((organizer packageNamed: packageName ifAbsent: [ ]) definedClasses collect: [ :each | each name ]) ].
	dependentClassNames do: [ :depClassName | "In the past this fetcher was handling the global namespace search by removing the 
	already seen classes from the namespace. In fact this is not needed because 
	the heuristics handles the repettion of proposition. 
	So here we default to the configuration and following fetcher."
			aBlock value: ((CoGlobalEntry contents: depClassName node: nil)
					 fetcherName: self class name;
					 yourself) ]
]
