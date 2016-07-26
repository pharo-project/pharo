rewrite code
sourceRegexText
	^ 'RBSourceRegexRefactoring new
	
	"Example 1: Replace symbols with strings"
	replace: ''#(\w+)'' with: ''''''$1'''''' ignoreCase: false;
	
	"Example 2: Replace 4 spaces with tabs"
	replace: ''    '' with: ''	'' ignoreCase: false;

	yourself'