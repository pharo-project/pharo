rewrite code
classRegexText
	^ 'RBClassRegexRefactoring new

	"Example 1: Change class prefixes"
	renameClasses;
	replace: ''^AB(.*)$'' with: ''CD$1'' ignoreCase: false;

	"Example 2: Generate empty test classes"
	createClasses;
	rootClass: TestCase;
	replace: ''^.*$'' with: ''$0Test'' ignoreCase: false;

	"Example 3: Copy classes"
	copyClasses;
	replace: ''^.*$'' with: ''$0Plus'' ignoreCase: false;

	yourself'