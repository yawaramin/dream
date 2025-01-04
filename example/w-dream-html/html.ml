let greeting = Dream_html.path "/greeting/%s" "/greeting/%s?src=%s"
let whoops = Dream_html.path "/whoops" "/whoops"
let par = Dream_html.path "/par" "/par"

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

let test_par env _ =
  0.1
  |> Array.make 10
  |> Par.sum env
  |> Printf.sprintf "%.16f"
  |> Dream.html

let () =
  Random.self_init ();
  Par.run @@ fun env ->

  Dream.run env
  @@ Dream.logger
  @@ Dream.router [
    Dream_html.get greeting greet;
    Dream_html.get whoops implode;
    Dream_html.get par (test_par env);
    Dream.get "/static/**" (Dream.static (Eio.Stdenv.cwd env));
  ]
