examples - cleanup
closeExampleDialogs
	"Close the example dialogs."
	"self closeExampleDialogs"
	<script>
	DialogWindow allSubInstances do: [:d | d cancel]