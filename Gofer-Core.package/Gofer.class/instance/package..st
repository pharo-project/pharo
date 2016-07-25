references
package: aString
	"Add the package aString to the receiver. aString is a package name as it appears in the Monticello Browser, something like 'Magritte-Seaside'. Gofer will try to resolve this name to an actual version in one of the configured repositories before doing something with the package. Gofer sorts all the versions in all the repositories according to branch name (versions without a branch are preferred), version number, author name and repository priority. The top hit of this sorted list is eventually going to be loaded and used."

	references addLast: (GoferPackageReference name: aString)