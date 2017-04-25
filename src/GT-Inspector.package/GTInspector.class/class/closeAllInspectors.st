utilities api
closeAllInspectors
	<script>
	"self closeAllInspectors"
	(SystemWindow allSubInstances select: [:w | 
		 w model isKindOf: self])
			do: [:w | w delete ].
