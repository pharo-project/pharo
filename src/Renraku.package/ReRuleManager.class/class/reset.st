actions
reset
	<script>
	uniqueInstance ifNotNil: [ uniqueInstance unsubscribe ].
	uniqueInstance := nil