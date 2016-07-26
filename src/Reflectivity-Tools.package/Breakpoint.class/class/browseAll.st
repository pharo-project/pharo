all
browseAll
	<script>
	| methods |
	methods := AllBreakpoints flatCollect: [ :breakpoint | breakpoint link methods ].
	Smalltalk tools messageList browse: methods