publishing
publishHTMLHeader
	self stream
		<< '<!DOCTYPE html>'; lf;
		<< '<html lang="en">'; lf;
		<< '<head>'; lf;
		<< '<title>Dependencies Report: </title>'; lf.

	self styleSheet: 'https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css'.
	self styleSheet: 'https://cdn.datatables.net/1.10.8/css/jquery.dataTables.min.css'.
	self javaScript: 'https://code.jquery.com/jquery-1.11.3.min.js'.
	self javaScript: 'https://cdn.datatables.net/1.10.8/js/jquery.dataTables.min.js'.
	self javaScript: 'https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js'.
			
	stream << '<script type="text/javascript" class="init">
$(document).ready(function() {
	$(''#example'').DataTable( {
		"paging": false,
		"aoColumns": [ null, { "bSearchable": false }, { "bSearchable": false } ]
	});
} );
	</script>'.
		
	self stream
		lf;
		<< '</head>'; lf;
		<< '<body>'; lf;
		<< '<div class="container">'; lf;
		<< '<h1>Dependency analysis</h1>'; lf.