cleanup
cleanUp
	<script: 'self new cleanUp'>
	Link ifNotNil: [ :link | link uninstall. Link := nil ]