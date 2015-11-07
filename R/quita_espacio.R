quita_espacio <-
function(cadena)
{
	unlist(lapply(strsplit(cadena," "),function(x)paste(x[1:length(x)],collapse="")))
}
