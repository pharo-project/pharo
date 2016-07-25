snapshotting-codegen
symbolicSourceFor: versionName

	^ String streamContents: [:stream |
		stream
			<< 'stable: spec';
			cr;
			<< '<symbolicVersion: #''stable''>';
			cr;
			<< 'spec';
			cr.
			
			self supportedVersions
				do: [ :v |
					stream tab;
						<< 'for: #''pharo';
						<< v;
						<< '.x'' version: ';
						<< versionName surroundedBySingleQuotes ]
				separatedBy: [
					stream << ';'; cr] ]