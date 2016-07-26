private
package 
	MCWorkingCopy managersForClass: MFClassA  do: [:p | ^ p packageSet packages first].
	" should be equivalent to RPackageOrganizer default packageNamed: #'Manifest-Resources-Tests' "