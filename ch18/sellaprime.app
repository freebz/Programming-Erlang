%% '기본(base)' 애플리케이션에 대한 애플리케이션 리소스 파일(.app 파일)
{application, sellaprime,
 [{description, "The Prime Number Shop" },
  {vsn, "1.0" },
 {modules, [sellaprime_app, sellaprime_supervisor, area_server,
 	    prime_server, lib_primes, my_alarm_handler]},
 {registered,[area_server, prime_server, sellaprime_super]},
 {applications, [kernel,stdlib]},
 {mod, {sellaprime_app,[]}},
 {start_phases, []}
]}.
