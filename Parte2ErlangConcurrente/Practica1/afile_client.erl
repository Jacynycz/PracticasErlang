-module(afile_client).
-export([ls/1, get_file/2, cd/2]).
ls(Server) ->
  Server ! {self(), list_dir},
  receive
  {Server, FileList} ->
  FileList
end.

get_file(Server, File) ->
  Server ! {self(), {get_file, File}},
  receive
  {Server, Content} ->
  Content
end.

cd(Server, AbsolutePath) ->
Server ! {self(), {change_dir, AbsolutePath}},
receive
{Server, Content} ->
Content
end.
