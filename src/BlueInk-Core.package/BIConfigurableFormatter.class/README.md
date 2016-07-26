BIConfigurableFormatter formats the Refactoring Browser's parse trees. It has many more formatting options than the default formatter used by the RB. To change the RB to use this formatter, execute "RBProgramNode formatterClass: BIConfigurableFormatter". For some refactorings the RB must reformat the code after the change, so it is good to have a formatter configured to your tastes.

Instance Variables:
	codeStream	<PositionableStream>	the stream we are writing our output to
	indent	<Integer>	how many times are we indenting a new line -- indents are normally tabs but could be any whitespace string
	lineStart	<Integer>	the position of the character that started the current line. This is used for calculating the line length.
	lookaheadCode	<Dictionary key: RBProgramNode value: String>	sometimes we need to lookahead while formatting, this dictionary contains the nodes that have already been formatted by lookahead
	originalSource	<String>	the original source before we started formatting. This is used to extract the comments from the original source.

BIConfigurableFormatter new