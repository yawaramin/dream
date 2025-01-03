let greeting = Dream_html.path "/greeting/%s" "/greeting/%s?src=%s"
let whoops = Dream_html.path "/whoops" "/whoops"

let greet _request who =
  let open Dream_html in
  let open HTML in
  respond @@ html [lang "en"] [
    head [] [
      title [] "Greeting";
    ];
    comment "HTML comment";
    body [] [
      h1 [] [txt "Good morning, %s!" who];
      p [] [a [path_attr href greeting who "m"] [txt "You are here"]];
    ];
  ]

let implode _request = failwith "Oh no!"

let () =
  Par.run @@ fun env ->

  if Par.id () = 0 then
    Printf.printf "\n%f\n%!" (Eio.Promise.await (Par.sum env [|
      23.; 54.; 456.4; 547.4; 7456.4; 674.23; 6453.34; 5345.65; 67456.353;
      64567.23; 6546.6; 4357.4;
    |]));

  Dream.run env
  @@ Dream.logger
  @@ Dream.router [
    Dream_html.get greeting greet;
    Dream_html.get whoops implode;
    Dream.get "/static/**" (Dream.static (Eio.Stdenv.cwd env));
  ]
