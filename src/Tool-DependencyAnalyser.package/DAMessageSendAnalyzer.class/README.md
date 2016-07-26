I analyze package sent messages to reveal dependencies.

Examples:
(self on: 'Kernel') difference
(self on: 'Kernel') missingMethodsWithPotentialMatch
(self on: 'Kernel') possibleDeadCode
(self on: 'Kernel') missingDependencies
(self on: 'Kernel') 	missingMethodsWithPotentialMatchAfterMissingDependenciesAddition
