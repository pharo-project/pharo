html utilities
accordion: id name: name headingContent: heading body: body
	stream 
		<< '<div class="panel-group" id="' << id << '" role="tablist" aria-multiselectable="true">'.

	stream << '  <div class="panel panel-default">
    <div class="panel-heading" role="tab" id="heading' << name << '">
      <h4 class="panel-title">
        <a role="button" data-toggle="collapse" data-parent="#' << id << '" href="#collapse' << name << '" aria-expanded="true" aria-controls="collapse' << name << '">' ; lf.
	stream
		<< heading
<< '
        </a>
      </h4>
    </div>
    <div id="collapse' << name << '" class="panel-collapse collapse in" role="tabpanel" aria-labelledby="heading' << name << '">
      <div class="panel-body">
      		'.
	stream 
		<< body
		<< '
      </div>
    </div>
  </div>'.
		
	stream
		<< '</div>'; lf