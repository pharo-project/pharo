snapshotting-codegen
confSourceFor: versionName with: versions

	^ String streamContents: [ :stream |
		stream
			<< 'version';
			<< (versionName copyReplaceAll: '.' with: '_');
			<< ': spec';
			cr;
			<< '<version: ';
			<< 	versionName surroundedBySingleQuotes;
			<<	'>';
			cr;
			<< 'spec for: #common do: [';
			cr;
			<< 'spec blessing: #release;';
			cr;
			<< 'repository: ';
			<< self mainRepo location surroundedBySingleQuotes;
			<< '.';
			cr;
			<< 'spec';
			cr.
			
		versions do: [ :v |
			stream tab;
				<< 'package: ';
				<< v package name surroundedBySingleQuotes;
				<< ' with: ';
				<< v info name surroundedBySingleQuotes ]
			separatedBy: [
				stream << ';'; cr].

		stream
			<< $.; cr;
			<< 'spec postLoadDoIt: #postload'.	
				
				
      stream << ' ]' ]