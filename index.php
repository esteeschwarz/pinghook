<html>
    <style>

pre {
    display: block;
    padding: 9.5px;
    margin: 0 0 10px;
    font-size: 13px;
    line-height: 1.42857143;
    color: #333;
    word-break: break-all;
    word-wrap: break-word;
    background-color: #f5f5f5;
    border: 1px solid #ccc;
    border-radius:4px
}

    </style>
    <body>
        <p>snc</p>
        <pre>
        <?php
$output = [];
$return_var = 0;
exec('git pull 2>&1', $output, $return_var);
if ($return_var !== 0) {
    echo "Error pulling from repository:\n";
    echo implode("\n", $output);
} else {
    echo "Repository successfully updated:\n";
    echo implode("\n", $output);
}
?>
</pre>
        <pre>
            1.git remote, tapee
            2.git remote add mini-pinghook http://server/pinghook.git
            15511.1 clone as www
            2. sh php pull as www: wks.
            3. http php pull: no.
            4. permissions set: still skw. in sh
            5. not http.
        </pre>
    </body>
</html>