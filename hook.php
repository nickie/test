<?php
  $GIT_RO="/home/nickie/Projects/release/test";
  if ($_POST['payload'] && $_POST['magic'] == "42f17bad") {
    error_reporting(0);
    try {
      $payload = json_decode($_POST['payload']);
    }
    catch(Exception $e) {
      exit "Invalid payload!";
    }
    if ($payload->ref != 'refs/heads/master') {
      exit "I only care about the master branch!"; 
    }
    shell_exec('cd $GIT_RO && git reset --hard HEAD && git pull && make hook');
  }
  else {
    exit "This should only be used as a hook for github/gitlab."
  }
?>
