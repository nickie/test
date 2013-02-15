<?php
  error_reporting(0);
  $GIT_MAGIC="42f17bad";
  $GIT_RO="/home/nickie/Projects/release/test";
  
  if ($_POST['payload'] && $_POST['magic'] == $GIT_MAGIC) {
    try {
      $payload = json_decode($_POST['payload']);
    }
    catch(Exception $e) {
      exit("Invalid payload!");
    }
    if ($payload->ref != 'refs/heads/master') {
      exit("I only care about the master branch!"); 
    }
  }
  elseif (!isset($_GET['force'])) {
    exit("This should only be used as a hook for github/gitlab.");
  }
  shell_exec("cd $GIT_RO && git reset --hard HEAD && git pull && make hook");
?>
