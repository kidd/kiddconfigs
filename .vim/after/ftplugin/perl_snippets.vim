if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

"exec "Snippet nperl  #!/usr/bin/perl<cr><cr>use strict;<cr>use warnings;<cr>use Data::Dumper;<cr><cr>".st.et
"exec "Snippet ii  open \"<".st."$fh".et"\" or die "Cannot open".st."$fh".et."$!\n\""

exec "Snippet sub sub ".st."FunctionName".et." {<CR>my (".st.et.") = @_;<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet class package ".st."ClassName".et.";<CR><CR>".st.et.st."ParentClass".et.st.et.";<CR><CR>sub new {<CR>my \$class = shift;<CR>\$class = ref \$class if ref \$class;<CR>my $self = bless {}, \$class;<CR>\$self;<CR>}<CR><CR>1;<CR>".st.et
exec "Snippet xfore ".st."expression".et." foreach @".st."array".et.";".st.et
exec "Snippet xwhile ".st."expression".et." while ".st."condition".et.";".st.et
exec "Snippet xunless ".st."expression".et." unless ".st."condition".et.";".st.et
exec "Snippet slurp my $".st."var".et.";<CR><CR>{ local $/ = undef; local *FILE; open FILE, \"<".st."file".et.">\"; $".st."var".et." = <FILE>; close FILE }".st.et
exec "Snippet if if (".st.et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet unless unless (".st.et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet ifee if (".st.et.") {<CR>".st.et."<CR><BS>} elsif (".st.et.") {<CR>".st.et."<CR><BS>} else {<CR>".st.et."<CR>}<CR><CR>".st.et
exec "Snippet ife if (".st.et.") {<CR>".st.et."<CR>} else {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet for for (my \$".st."var".et." = 0; \$".st."var".et." < ".st."expression".et."; \$".st."var".et."++) {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet fore for (".st."array".et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet eval eval {<CR>".st.et."<CR>};<CR>if ($@) {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet while while (".st.et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet xif ".st."expression".et." if ".st."condition".et.";".st.et
exec "Snippet pod =head2 ".st."funname".et."<CR><CR>=over 4<CR><CR>=item Arguments:<CR><CR>=back<CR><CR>=cut<CR>".st.et

"Catalyst
exec "Snippet csub sub ".st.et." : ".st.et." {<CR>    my (\$self, \$c,) = @_;<CR><CR>    ".st.et."<CR>}<CR>"
exec "Snippet param $c->req->param(".st.et.")"
exec "Snippet stash $c->stash->{".st.et."}"
exec "Snippet conf $c->config->{".st.et."}"
exec "Snippet ses $c->session->{".st.et."}"

exec "Snippet haserr $c->form->has_error".st.et
exec "Snippet seterr $c->set_invalid_form(".st.et.");"

exec "Snippet fw $c->forward('".st.et."');"
exec "Snippet dt $c->detach('".st.et."');"

exec "Snippet model $c->model('".st.et."');"
exec "Snippet find $c->model('".st.et."')->find(".st.et.");"
exec "Snippet search $c->model('".st.et."')->search({".st.et."},);"
exec "Snippet count $c->model('".st.et."')->count({".st.et."},);"
exec "Snippet create $c->model('".st.et."')->create({".st.et."});"

exec "Snippet dump $c->log->dumper([".st.et."]);"

