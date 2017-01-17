{application, azuqua_erl, [
	{description, "Erlang client for the Azuqua API"},
	{vsn, "0.0.1"},
	{modules, ['azq_api','azq_api_bin','azq_api_utils']},
	{registered, []},
	{applications, [kernel,stdlib,jiffy,hackney,lager,edown]}
]}.